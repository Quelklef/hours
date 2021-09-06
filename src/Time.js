exports.getNow_f = mk => () => mk(+Date.now());

exports.isToday = t0 => () => {
        t0 = new Date(t0.millis);
  const t1 = new Date();

  return (
       t0.getYear()  === t1.getYear()
    && t0.getMonth() === t1.getMonth()
    && t0.getDate()  === t1.getDate()
  );
};

exports.parseMinutes_f = str => {
  const match = str.match(/^(?:([0-9]+)h ?)?(?:([0-9]+)m)?$/);
  if (match === null) return null;
  return parseInt(match[1] || '0') * 60 + parseInt(match[2] || '0');
};

exports.printMillis = ms =>
  new Intl.DateTimeFormat('en-US', { dateStyle: 'long', timeStyle: 'long', hour12: false }).format(ms);

exports.parseMillis = s => {
  const ms = +new Date(s);
  return Number.isNaN(ms) ? null : ms;
};
