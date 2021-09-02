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
