const { spawn } = require('child_process');
const fs = require('fs');

exports.invokeEditor = floc => () => {
  return new Promise((resolve, reject) => {
    const editor = process.env.VISUAL || process.env.EDITOR || 'vi';
    const cmd = spawn(editor, [floc], { stdio: 'inherit' });
    cmd.on('exit', () => resolve());
  });
};

exports.prettifyJSON = jsonStr => () => {
  return JSON.stringify(JSON.parse(jsonStr), null, 2);
}
