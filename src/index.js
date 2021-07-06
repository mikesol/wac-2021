import "./style.css";
var main = require("../output/Main/index.js");
main.main();

if (module.hot) {
    module.hot.accept("./index.js", function() {});
    module.hot.accept("../output/Intro.Main/index.js", function() { console.log("reload main"); main.main(); });
    module.hot.accept("../output/Main/index.js", function() { console.log("reload idx"); main.main(); });
    module.hot.accept("../output/Example1.Main/index.js", function() { console.log("reload"); main.main(); });
    module.hot.accept("../output/Example2.Main/index.js", function() { console.log("reload"); main.main(); });
    module.hot.accept("../output/Example3.Main/index.js", function() { console.log("reload"); main.main(); });
}