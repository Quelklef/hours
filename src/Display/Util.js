exports.displayMillisecondsAsDateTime = ms =>
  new Intl.DateTimeFormat('en-US', { dateStyle: 'long', timeStyle: 'long', hour12: false }).format(ms);

exports.displayMillisecondsAsDate = ms =>
  exports.displayMillisecondsAsDateTime(ms).split(', ').slice(0, 2).join(', ');

exports.displayMillisecondsAsHHMM = ms =>
  ('' + new Date(ms).getHours()).padStart(2, '0') + ":" + ('' + new Date(ms).getMinutes()).padStart(2, '0');
