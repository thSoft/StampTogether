///<reference path="../../build/typings/tsd.d.ts" />

interface Elm {
  StampTogether: ElmModule<StampTogetherPorts>;
}

interface Stamp {
  url: string;
  value: {
    x: number;
    y: number;
  }
}

interface StampTogetherPorts {
  observedUrls: PortFromElm<Array<string>>;
  state: PortToElm<Array<Stamp>>;
  createStamp: PortFromElm<FireElm.Data>;
  deleteStamp: PortFromElm<string>;
}

window.onload = () => {
  var component = Elm.fullscreen(Elm.StampTogether, {
    state: []
  });
  FireElm.read(component.ports.observedUrls, component.ports.state, makeStamps);
  FireElm.push(component.ports.createStamp);
  FireElm.remove(component.ports.deleteStamp);
}

function makeStamps(snapshot: FirebaseDataSnapshot): Array<Stamp> {
  var result: Array<Stamp> = [];
  snapshot.forEach(child => {
    var value = child.val();
    result.push({
      url: child.ref().toString(),
      value: {
        x: value.x,
        y: value.y
      }
    });
  });
  return result;
}