"use strict";

var GROUP_ID_IDX = 0;
var GROUP_NAME_IDX = 1;
var GROUP_ALIAS_IDX = 2;
var POST_ID_IDX = 3;
var POST_SUBJECT_IDX = 4;
var POST_DATE_IDX = 5;
var POST_FROM_IDX = 6;

var renderGroupSummary = function(groupSummary) {
    var groupRow = $('<div class="row"></div>');
    var summaryRow = $('<div class="row"></div>');
    var groupName = groupSummary[GROUP_NAME_IDX];
    var groupAlias = groupSummary[GROUP_ALIAS_IDX];

    var postId = groupSummary[POST_ID_IDX];
    var postSubject = groupSummary[POST_SUBJECT_IDX];
    var unixUtcPostDate = groupSummary[POST_DATE_IDX];
    var postFrom = groupSummary[POST_FROM_IDX];
    var groupsLink = '/group/' + groupAlias;
    var groupsMailLink = 'mailto:' + groupAlias + '@kobbweb.net';

    groupRow.append('<div class="col-md-10"><a href="' + groupsLink + '"><strong>' + groupName + '</strong></a></div>');
    groupRow.append('<div class="col-md-2"><a href="' + groupsMailLink + '">' + groupAlias + '@kobbweb.net</a></div>');

    if (postId != null) {
        var postDate = dateFromUnixUtc(unixUtcPostDate);
        var postDateString = formatDateString(postDate);

        summaryRow.append('<div class="col-md-8">' + postSubject + '</div>');
        summaryRow.append('<div class="col-md-2">' + postFrom + '</div>');
        summaryRow.append('<div class="col-md-2">' + postDateString + '</div>');
    } else {
        summaryRow.append('<div class="col-md-12">No content has been sent to this group!</div>');
    }

    return [groupRow, summaryRow];
}

var onSuccessCallback = function(data, status, xhr) {
    var groupsSummaries = data;

    groupsSummaries.sort(function(a, b) {
        var name1 = a[GROUP_NAME_IDX];
        var name2 = b[GROUP_NAME_IDX];
        return name1.localeCompare(name2);
    });

    var el = $('<div class="container"></div>');

    for (var i = 0; i < groupsSummaries.length; ++i) {
        var groupSummary = groupsSummaries[i];
        var renderedSummaries = renderGroupSummary(groupSummary);
        el.append(renderedSummaries[0]);
        el.append(renderedSummaries[1]);
    }

    $('#app').append(el);
};

var onFailureCallback = function(xhr, status, error) {
    // TODO: implement.
};

var init = function() {
    var ajaxOptions = {
        url : '/api/groups',
        type : 'GET',
        dataType : 'json',
        success : onSuccessCallback,
        error: onFailureCallback
    };

    $.ajax(ajaxOptions);
};

