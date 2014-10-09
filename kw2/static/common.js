"use strict";

var dateFromUnixUtc = function(unixTimestamp) {
    return new Date(Date.UTC(0, 0, 0, 0, 0, unixTimestamp, 0));
};

var zeroPad = function(x) {
    if (x < 10) {
        return '0' + x;
    }

    return '' + x;
};

var formatDateString = function(date) {
    return '' + date.getFullYear() + '-' + zeroPad(date.getMonth()) + '-' + zeroPad(date.getDate()) + ' ' + zeroPad(date.getHours()) + ':' + zeroPad(date.getMinutes());
};

