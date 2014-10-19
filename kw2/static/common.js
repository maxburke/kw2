"use strict";

var dateFromUnixUtc = function(unixTimestamp) {
    var hours = unixTimestamp[0];
    var mins = unixTimestamp[1];
    var seconds = unixTimestamp[2];

    return new Date(Date.UTC(0, 0, 1, hours, mins, seconds, 0));
};

var zeroPad = function(x) {
    if (x < 10) {
        return '0' + x;
    }

    return '' + x;
};

var formatDateString = function(date) {
    var month = date.getMonth() + 1;
    return '' + date.getFullYear() + '-' + zeroPad(month) + '-' + zeroPad(date.getDate()) + ' ' + zeroPad(date.getHours()) + ':' + zeroPad(date.getMinutes());
};

