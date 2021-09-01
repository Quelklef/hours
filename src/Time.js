exports.getNow_f = mk => () => mk(+Date.now());

exports.prettifyMillis = ms =>
  new Intl.DateTimeFormat('en-US', { dateStyle: 'long', timeStyle: 'long', hour12: false }).format(ms);
