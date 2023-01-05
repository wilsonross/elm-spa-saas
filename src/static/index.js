var Elm = require("../elm/Main");
var storageKey = "store";
var flags = localStorage.getItem(storageKey);
var app = Elm.Main.init({ flags: flags });
