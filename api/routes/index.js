const express = require("express");
const router = express.Router();
const randomstring = require("randomstring");
const mongoose = require("mongoose");
const MultivariateNormal = require("multivariate-normal").default;
// console.log(MultivariateNormal);
// const csv = require("csv-parser");
const fs = require("fs");
const math = require("mathjs");

// console.log(__dirname);
let rawdata = fs.readFileSync("./public/data/finalVariables.json");
let jsonData = JSON.parse(rawdata);
// console.log(jsonData);
// console.log(jsonData);

let dataDict = {};

jsonData.forEach((d) => {
  dataDict[`${d["vars"][0][0]}_${d["vars"][0][1]}`] = d;
});

// console.log(dataDict);

// let dataList = Object.keys(jsonData).map(function (d) {
//   return jsonData[d];
// });

let dataList = jsonData;

//states are whether it is initial elicitation, data visualization, or secondary elicitation
// visgroups are line band hop and scatter, self explanatory

// let set1 = dataList.filter(function (d) {
//   return d["set"] === 1;
// });
// let set2 = dataList.filter(function (d) {
//   return d["set"] === 2;
// });

// let datasets = [set1, set2];

// console.log(datasets);

let variables = jsonData.map((d) => {
  return d["vars"];
});
// console.log(variables);
// console.log(variables);

let states = ["draw1", "dataViz", "draw2"];
let visGroups = ["line", "band", "hop"];

let visGroupCounts = {
  line: 0,
  band: 0,
  hop: 0,
};

const maxEachGroup = 125;

// let variables = Object.keys(jsonData).map(function(d) {
//   return jsonData[d]["vars"];
// });
// console.log(variables);

const url =
  "mongodb://markant:emotion2019@ds159025.mlab.com:59025/markantstudy";

mongoose.connect(url);
mongoose.promise = global.Promise;

// const db = mongoose.anchoring;

const Schema = mongoose.Schema;

const responseSchema = new Schema({
  usertoken: {
    type: String,
    required: true,
    unique: true,
  },
  variables: Schema.Types.Array,
  participantGroup: String,
  visGroup: String,
  date: {
    type: Date,
    default: Date.now,
  },
  attention: Schema.Types.Mixed,
  lineTestErrors: Schema.Types.Mixed,
  bandTestErrors: Schema.Types.Mixed,
  prequestionnaire: Schema.Types.Mixed,
  postquestionnaire: Schema.Types.Mixed,
  responses: Schema.Types.Array,
  paid: { type: Boolean, Defult: false },
});

const countSchema = new Schema({
  line: { type: Number, default: 0 },
  band: { type: Number, default: 0 },
  hop: { type: Number, default: 0 },
});

const visGroupSchema = new Schema({
  mturk: countSchema,
  class: countSchema,
  lab: countSchema,
});

const VisGroupCount = mongoose.model(
  "visGroupCount",
  visGroupSchema,
  "visGroupCounts"
);

const Response = mongoose.model("confstudy3", responseSchema);

router.get("/api/consent/mturk", function (req, res) {
  // 0 is low 1 is high 2 is control //
  // for order 0 is basic anchoring first, then with map visualization and 1 is map visualization first and then basic anchoring//

  if (!req.session.userid) {
    let token = randomstring.generate(8);

    //signifies whether we are in the scatter plot stage or uncertainty stage
    req.session.uncertainty = false;
    //signifies wether we start with dataset 1 or dataset 2 (political or not)
    req.session.levelIndex = 0;
    // this will change to the actual group later. Might be a better way of doing this.

    // req.session.visGroup = "line";
    //user's unique token
    req.session.userid = token;
    req.session.completed = false;
    req.session.postQuestion = false;
    req.session.preQuestion = false;
    //this get incremented and iterates over different variables
    req.session.varIndex = 0;
    //this assigns the orders of variables.
    req.session.variables = shuffle(variables);
    //this assigns the state of the study i.e. elicitation 1, data vis, elicitation 2
    req.session.stateIndex = 0;
    req.session.participantGroup = "mturk";
    //this assigns a string format of state i.e. draw1, datavis, draw2
    req.session.state = states[req.session.stateIndex];

    let visgroupcounts = Response.aggregate([
      { $match: { responses: { $size: 48 } } },
      { $group: { _id: "$visGroup", count: { $sum: 1 } } },
    ]).exec();

    visgroupcounts.then((result) => {
      result.forEach((v) => {
        visGroupCounts[v["_id"]] = v["count"];
      });

      console.log(visGroupCounts);

      visGroups = Object.keys(visGroupCounts).filter((key) => {
        return visGroupCounts[key] <= maxEachGroup;
      });

      if (visGroups.length === 0) {
        visGroups = ["line", "band", "hop"];
      }

      console.log(visGroups);

      req.session.visGroup = visGroups[getRandomInt(visGroups.length)];
      // req.session.visGroup = "line";

      console.log(req.session.visGroup);

      let newResponse = new Response({
        usertoken: token,
        variables: req.session.variables,
        visGroup: req.session.visGroup,
        participantGroup: "mturk",
      });

      newResponse.save(function (err) {
        if (err) console.log(err);
        res.send({
          user: token,
        });
      });
    });
  } else {
    res.send("consent already given");
  }
});

//returns users token
router.get("/api/userinfo", function (req, res) {
  if (req.session.userid) {
    res.json({
      token: req.session.userid,
    });
  } else {
    res.send("please give consent first");
  }
});

//returns required data for running the expriment.
router.get("/api/data", function (req, res) {
  let vars = req.session.variables[req.session.varIndex];
  var dataset = dataDict[`${vars[0][0]}_${vars[0][1]}`];
  var congruent = dataset["congruent"];
  var nData = dataset["uncertainty"];
  // console.log(dataset);
  var dataGenerate = generateData(req.session.preRho, congruent, nData);
  // var dataGenerate = generateDataAlt(req.session.preRho);
  let varNumber = req.session.varIndex + 1 + req.session.levelIndex * 5;
  let d = {
    state: req.session.state,
    vars: vars[0],
    unit: vars[1],
    // data: dataset.data.data,
    congruent: congruent,
    uncertainty: nData,
    data: dataGenerate,
    rho: dataset.rho,
    N: dataset.N,
    visGroup: req.session.visGroup,

    varNumber: varNumber,
  };
  res.status(200).send(d);
});

//saves the current state and the variables as well as the responses.
router.post("/api/study", function (req, res) {
  let token = req.session.userid;
  let data = req.body;
  console.log(data);
  console.log(req.session.stateIndex);
  if (req.session.stateIndex === 0) {
    req.session.preRho = parseFloat(data["belief"]);
    req.session.preUncertainty = data["uncertainty"];
  }

  data["state"] = states[req.session.stateIndex];
  data["variables"] = req.session.variables[req.session.varIndex];

  Response.findOneAndUpdate(
    { usertoken: token },
    {
      $push: { responses: data },
    },
    function (err, doc) {
      if (err) {
        return res.send(500, { error: err });
      }
      return res.send(200, `successfully saved study`);
    }
  );
});

router.post("/api/linetest", function (req, res) {
  let token = req.session.userid;
  let data = req.body;
  // console.log(data);
  console.log(data);
  Response.findOneAndUpdate(
    { usertoken: token },
    {
      lineTestErrors: data,
    },
    function (err, doc) {
      if (err) return res.send(500, { error: err });
      console.log("yeaah");
      return res.send("successfully saved!");
    }
  );
});

router.post("/api/bandtest", function (req, res) {
  let token = req.session.userid;
  let data = req.body;
  // console.log(data);
  console.log(data);
  Response.findOneAndUpdate(
    { usertoken: token },
    {
      bandTestErrors: data,
    },
    function (err, doc) {
      if (err) return res.send(500, { error: err });
      console.log("yeaah");
      return res.send("successfully saved!");
    }
  );
});

router.post("/api/attention", function (req, res) {
  let token = req.session.userid;
  let data = req.body;
  // console.log(data);
  Response.findOneAndUpdate(
    { usertoken: token },
    {
      attention: data,
    },
    function (err, doc) {
      if (err) return res.send(500, { error: err });
      console.log("yeaah");
      return res.send("successfully saved!");
    }
  );
});

//prequestionaire
router.post("/api/pre", function (req, res) {
  let token = req.session.userid;
  let data = req.body;
  // console.log(data);
  Response.findOneAndUpdate(
    { usertoken: token, prequestionnaire: { $exists: false } },
    {
      prequestionnaire: data,
    },
    function (err, doc) {
      if (err) return res.send(500, { error: err });
      // console.log("yeaah");
      req.session.preQuestion = true;
      return res.send("successfully saved!");
    }
  );
});

//post questionaire
router.post("/api/post", function (req, res) {
  let token = req.session.userid;
  let data = req.body;
  // console.log(data);
  Response.findOneAndUpdate(
    { usertoken: token, postquestionnaire: { $exists: false } },
    {
      postquestionnaire: data,
    },
    function (err, doc) {
      if (err) return res.send(500, { error: err });
      // console.log("yeaah");
      req.session.completed = true;
      console.log("ASdasd");
      return res.send("successfully saved!");
    }
  );
});

//first page
router.get("/", function (req, res) {
  if (req.session.completed) {
    res.render("debriefMTurk.html");
  } else {
    res.redirect("consent/mturk");
  }
});
// consent page
router.get("/consent/mturk", function (req, res) {
  if (req.session.completed) {
    res.render("debriefMTurk.html");
  } else {
    res.render("consentMTurk.html");
  }
});

router.get("/intermission", function (req, res) {
  if (!req.session.userid) {
    res.redirect("/consent/mturk");
  } else {
    if (req.session.varIndex === variables.length && !req.session.uncertainty) {
      // req.session.completed = true;
      res.redirect("/next");
    } else if (
      req.session.varIndex === variables.length &&
      req.session.uncertainty
    ) {
      req.session.completed = true;
      res.redirect("/postforms");
    } else {
      res.render("intermission.html");
    }
  }
});

router.get("/instructions/correlation", function (req, res) {
  if (req.session.completed) {
    res.render("debriefMTurk.html");
  } else {
    res.render("instructionsCorrelation.html");
  }
});

router.get("/instructions/task", function (req, res) {
  if (req.session.completed) {
    res.render("debrief.html");
  } else {
    res.render("instructionsTask.html");
  }
});

router.get("/instructions/uncertainty", function (req, res) {
  if (req.session.completed) {
    res.render("debriefMTurk.html");
  } else {
    if (req.session.visGroup === "line") {
      res.render("instructionsLine.html");
    } else if (req.session.visGroup === "band") {
      res.render("instructionsBand.html");
    } else if (req.session.visGroup === "hop") {
      res.render("instructionsHop.html");
    } else {
      res.send("error");
    }
  }
});

router.get("/instructions/scatter", function (req, res) {
  if (req.session.completed) {
    res.render("debriefMTurk.html");
  } else {
    res.render("instructionsScatter.html");
  }
});

router.get("/attention", function (req, res) {
  if (req.session.completed) {
    res.render("debriefMTurk.html");
  } else {
    res.render("attention.html");
  }
});

router.get("/test/line", function (req, res) {
  if (req.session.completed) {
    res.render("debriefMTurk.html");
  } else {
    res.render("testLine.html");
  }
});

router.get("/test/band", function (req, res) {
  if (req.session.completed) {
    res.render("debriefMTurk.html");
  } else {
    res.render("testBand.html");
  }
});

router.get("/instructions/draw", function (req, res) {
  if (req.session.completed) {
    res.render("debriefMTurk.html");
  } else {
    res.render("instructionsDraw.html");
  }
});

router.get("/preforms", function (req, res) {
  if (!req.session.completed) {
    res.render("preforms.html");
  }
});

router.get("/postforms", function (req, res) {
  res.render("postforms.html");
});

router.get("/study", function (req, res) {
  console.log(req.session.state);
  if (req.session.stateIndex === 0) {
    res.render("lineChartDraw.html");
  } else if (req.session.stateIndex === 1) {
    res.render("dataViz.html");
  } else if (req.session.stateIndex === 2) {
    res.render("lineChartDraw.html");
  } else {
    res.send("error!");
  }
});

router.get("/dataviz", function (req, res) {
  res.render("dataViz.html");
});

router.get("/next", function (req, res) {
  if (req.session.varIndex <= variables.length / 2) {
    if (req.session.stateIndex === 0) {
      req.session.stateIndex += 1;
      req.session.state = states[req.session.stateIndex];
      res.redirect("/study");
    } else if (req.session.stateIndex === 1) {
      req.session.stateIndex += 1;
      req.session.state = states[req.session.stateIndex];
      res.redirect("/study");
    } else if (req.session.stateIndex === 2) {
      req.session.varIndex += 1;
      req.session.stateIndex = 0;
      req.session.state = states[req.session.stateIndex];
      if (req.session.varIndex === parseInt(variables.length / 2)) {
        res.redirect("attention");
      } else {
        res.redirect("/intermission");
      }
    }
    // else {
    //   req.session.stateIndex = 0;
    //   req.session.state = states[req.session.stateIndex];
    //   req.session.levelIndex++;
    //   res.redirect("/attention");
    // }
  } else {
    if (
      req.session.varIndex < variables.length &&
      req.session.stateIndex === 0
    ) {
      req.session.stateIndex += 1;
      req.session.state = states[req.session.stateIndex];
      res.redirect("/study");
    } else if (
      req.session.varIndex < variables.length &&
      req.session.stateIndex === 1
    ) {
      req.session.stateIndex += 1;
      req.session.state = states[req.session.stateIndex];
      res.redirect("/study");
    } else if (
      req.session.varIndex < variables.length &&
      req.session.stateIndex === 2
    ) {
      req.session.varIndex += 1;
      req.session.stateIndex = 0;
      req.session.state = states[req.session.stateIndex];
      res.redirect("/intermission");
    } else {
      res.redirect("/postforms");
    }
  }
});

router.get("/debrief", function (req, res) {
  console.log(req.session.completed);
  if (req.session.completed) {
    res.render("debriefMTurk.html");
  } else {
    res.send("please complete the study first");
  }
});

function zip() {
  let args = [].slice.call(arguments);
  let shortest =
    args.length === 0
      ? []
      : args.reduce(function (a, b) {
          return a.length < b.length ? a : b;
        });

  return shortest.map(function (_, i) {
    return args.map(function (array) {
      return array[i];
    });
  });
}

function shuffle(array) {
  var currentIndex = array.length,
    temporaryValue,
    randomIndex;

  // While there remain elements to shuffle...
  while (0 !== currentIndex) {
    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }

  return array;
}

function getRandomInt(max) {
  return Math.floor(Math.random() * Math.floor(max));
}

async function getVisGroup(participantGroup) {
  let visGroups;
  let visGroup;
  let doc = await VisGroupCount.findOne(
    { [participantGroup]: { $exists: true } },
    { _id: 0 }
  ).exec();

  let gCounts = doc[participantGroup];
  gCounts = {
    line: gCounts["line"],
    band: gCounts["band"],
    hop: gCounts["hop"],
  };
  visGroups = Object.keys(gCounts).filter((key) => {
    return gCounts[key] < maxEachGroup;
  });

  if (visGroups.length === 0) {
    visGroups = ["hop", "band"];
  }
  visGroup = visGroups[getRandomInt(visGroups.length)];
  console.log(visGroup);
  countVisgroups(participantGroup, visGroup);
  return await visGroup;
}

function countVisgroups(participantGroup, visGroup) {
  console.log(`${participantGroup}.${visGroup}`);
  VisGroupCount.updateOne(
    { [participantGroup]: { $exists: true } },
    { $inc: { [`${participantGroup}.${visGroup}`]: 1 } },
    function (err, result) {
      if (err) {
        console.log("results not added");
      } else {
        console.log("counts added");
      }
    }
  );
}

function generateData(preRho = 0, congruent = false, n = 10) {
  let thresh = 0.75;
  let meanVector = [0, 0];
  let dataRho;
  if (!congruent) {
    if (preRho > 0.0) {
      dataRho = preRho - 1.0;
    } else {
      dataRho = preRho + 1.0;
    }
  } else {
    if (preRho > 0.5) {
      dataRho = preRho - 0.25;
    } else if (preRho > 0) {
      dataRho = preRho + 0.25;
    } else if (preRho > -0.5) {
      dataRho = preRho - 0.25;
    } else {
      dataRho = preRho + 0.25;
    }
  }
  // let dataRho = 0.5;
  console.log(`congruent: ${congruent}`);
  console.log(dataRho);
  if (dataRho > thresh) {
    dataRho = thresh;
  } else if (dataRho < -0.75) {
    dataRho = -thresh;
  }

  console.log(dataRho);
  console.log(preRho);

  let covarianceMatrix = [
    [1.0, dataRho],
    [dataRho, 1.0],
  ];
  let distribution = MultivariateNormal(meanVector, covarianceMatrix);
  let outData = [];
  for (let i = 0; i < n; i++) {
    sample = distribution.sample();
    outData.push({ x: sample[0], y: sample[1] });
  }
  let xAvg = math.mean(
    outData.map((o) => {
      return o.x;
    })
  );
  let yAvg = math.mean(
    outData.map((o) => {
      // console.log(o);
      return o.y;
    })
  );

  outData = outData.map((xy) => {
    return { x: xy["x"] - xAvg, y: xy["y"] - yAvg };
  });

  // console.log(outData);

  return outData;
}

// function generateDataAlt(preRho = 0, congruent = false, n = 100) {
//   let meanVector = [0, 0];
//   let dataRho;
//   if (!congruent) {
//     if (preRho > 0.0) {
//       dataRho = preRho - 1.0;
//     } else {
//       dataRho = preRho + 1.0;
//     }
//   } else {
//     dataRho = preRho + Math.random() / 5 - 0.1;
//   }
//   // let dataRho = 0.5;
//   console.log("congruent");
//   console.log(dataRho);
//   console.log(preRho);
//   let covarianceMatrix = [
//     [1.0, dataRho],
//     [dataRho, 1.0],
//   ];
//   let distribution = MultivariateNormal(meanVector, covarianceMatrix);
//   let outData = [];
//   for (let i = 0; i < n; i++) {
//     sample = distribution.sample();
//     outData.push({ x: sample[0], y: sample[1] });
//   }
//   return outData;
// }
// console.log(generateData());

module.exports = router;
