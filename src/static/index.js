import { Elm } from "../elm/Main";

const flags = localStorage.getItem("store");
const node = document.getElementById("main");

Elm.Main.init({
  flags: flags,
  node: node,
});
