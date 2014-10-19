"use strict";

var POST_ID_IDX = 0;
var PARENT_POST_ID_IDX = 1;
var MESSAGE_ID_IDX = 2;
var FROM_IDX = 3;
var SUBJECT_IDX = 4;
var DATE_IDX = 5;
var NUM_REPLIES_IDX = 6;

var extractGroupFromUri = function() {
    var uri = window.location.pathname;
    var components = uri.split('/');
    var numComponents = components.length;

    var group = components[numComponents - 1];

    return group;
};

var formatFrom = function(from) {
    var pattern = /^(.*)\s+<(.*)>/;
    var match = pattern.exec(from);

    if (match) {
        var name = match[1];
        var email = match[2];
        return '<a href="mailto:' + email + '">' + name + '</a>';
    } else {
        return from;
    }
};

var renderPostSummary = function(postSummary) {
    var postRow = $('<div class="row"></div>');

    var postId = postSummary[POST_ID_IDX];
    var subject = postSummary[SUBJECT_IDX];
    var from = postSummary[FROM_IDX];
    var unixUtcPostDate = postSummary[DATE_IDX];

    var formattedFrom = formatFrom(from);
    var date = dateFromUnixUtc(unixUtcPostDate);
    var formattedDate = formatDateString(date);

    var numReplies = postSummary[NUM_REPLIES_IDX];
    var lastPostInfo = formattedDate + ' by unknown';

    postRow.append('<div class="col-md-6">' + subject + '</div>');
    postRow.append('<div class="col-md-2">' + formattedFrom + '</div>');
    postRow.append('<div class="col-md-1">' + numReplies + '</div>');
    postRow.append('<div class="col-md-3">' + lastPostInfo + '</div>');

    return postRow;
};

var onSuccessCallback = function(data, status, xhr) {
    var postSummaries = data;

    postSummaries.sort(function(a, b) {
        var date1 = a[DATE_IDX];
        var date2 = b[DATE_IDX];

        return date1 - date2;
    });

    var el = $('<div class="container"></div>');

    for (var i = 0; i < postSummaries.length; ++i) {
        var postSummary = postSummaries[i];
        var renderedSummary = renderPostSummary(postSummary);
        el.append(renderedSummary);
    }

    $('#app').append(el);
}

var onFailureCallback = function(xhr, status, error) {
    // TODO: implement.
};

var init = function() {
    var group = extractGroupFromUri();
    var ajaxOptions = {
        url : '/api/group/summary/' + group,
        type : 'GET',
        dataType : 'json',
        success : onSuccessCallback,
        error : onFailureCallback
    };

    $.ajax(ajaxOptions);
};

