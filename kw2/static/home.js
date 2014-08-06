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
    var postDate = groupSummary[POST_DATE_IDX];
    var postFrom = groupSummary[POST_FROM_IDX];

    groupRow.append('<div class="col-md-10"><strong>' + groupName + '</strong></div>')
        .append('<div class="col-md-2"><a href="mailto:' + groupAlias + '@kobbweb.net' + '">' + groupAlias + '@kobbweb.net</a></div>');

    if (postId != null) {
        debugger;
    } else {
        summaryRow.append('<div class="col-md-12">No content has been sent to this group!</div>');
    }

    return [groupRow, summaryRow];
}

var onSuccessCallback = function(data, status, xhr) {
    var groupsSummaries = data;

    groupsSummaries.sort(function(a, b) {
        var name1 = a[1];
        var name2 = b[1];
        return name1.localeCompare(name2);
    });

    var el = $('<div class="container">');

    for (var i = 0; i < groupsSummaries.length; ++i) {
        var groupSummary = groupsSummaries[i];
        var renderedSummaries = renderGroupSummary(groupSummary);
        el.append(renderedSummaries[0]);
        el.append(renderedSummaries[1]);
    }

    $('#app').append(el);
};

var onFailureCallback = function(xhr, status, error) {
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

