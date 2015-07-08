var path = require('path');

exports.build = path.dirname(process.argv[1]);
exports.elmPackage = path.join(exports.build, 'elm-package.json');
var elmPackageContents = require(exports.elmPackage);

exports.root = path.join(exports.build, '..');

exports.sourceElm = path.join(exports.build, elmPackageContents['source-directories'][0]);
exports.rootModule = path.join(exports.sourceElm, elmPackageContents['exposed-modules'][0]);

exports.target = path.join(exports.root, 'target');
exports.targetMain = path.join(exports.target, 'main');

exports.sourceMainRoot = path.join(exports.rootModule, 'Main.elm')
exports.targetMainRoot = path.join(exports.targetMain, 'index.html')
