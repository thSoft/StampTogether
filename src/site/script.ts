///<reference path="../../build/typings/tsd.d.ts" />

interface Elm {
  StampTogether: ElmModule<StampTogetherPorts>;
}

interface Stamp {
  x: number;
  y: number;
}

interface StampTogetherPorts {
  observedUrls: PortFromElm<Array<string>>;
  state: PortToElm<FireElm.Data<Array<Stamp>>>;
  createStamp: PortFromElm<FireElm.Data<Stamp>>;
}

window.onload = () => {
  var component = Elm.fullscreen(Elm.StampTogether, {
    state: {
      url: "",
      value: []
    }
  });
  FireElm.read(component.ports.observedUrls, component.ports.state, objectToArray);
  FireElm.push(component.ports.createStamp);
}

function objectToArray(object: Object) {
  return Object.keys(object).map(key => object[key]);
}