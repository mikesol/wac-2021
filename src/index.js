import "./style.css";
var main = require("../output/Main/index.js");
main.main();

if (module.hot) {
    module.hot.accept();
}
  