import './style.css'
import { Elm } from './src/Main.elm?with=./src/Server.elm'

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: "Initial Message"
});

app.ports.connectWC.subscribe(function(uri) {
  console.log("connect wc", uri);
  connectWC(uri);
});
