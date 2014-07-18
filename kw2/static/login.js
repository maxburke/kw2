"use strict";

var spinner = null;

var submitDelegate = function(e, createNewUser) {
    $('#login-results').html('');

    var email = $('#email').val();
    var password = $('#password').val();
    var passwordVerify = $('#password-verify').val();

    if (createNewUser) {
        if (password.length < 6) {
            $('#login-results').html('<h4>Password must be at least 6 characters long!</h4>');
            return;
        } else if (password !== passwordVerify) {
            $('#login-results').html('<h4>Passwords do not match!</h4>');
            return;
        }
    }

    var spinnerOpts = {
        lines: 9,
        length: 7,
        width: 2,
        radius: 3,
        corners: 0.9,
        rotate: 0,
        direction: 1,
        color: '#000',
        speed: 1.2,
        trail: 45,
        shadow: false,
        hwaccel: false,
        className: 'spinnerClass',
        zIndex: 2e9,
        top: '50%',
        left:'50%' 
    };

    spinner = new Spinner(spinnerOpts).spin();
    $('#action-buttons').hide();
    $('#spinner-target').append(spinner.el);

    var ajaxOptions = {};
    var data = JSON.stringify({
        email : email,
        password : password,
        passwordVerify : passwordVerify
    });

    if (createNewUser) {
        ajaxOptions = {
            url : '/sessions',
            type : 'POST',
            data : data,
            dataType : 'json',
            processData : false,
            success : onNewUserSuccess,
            error : onNewUserFailure,
        };
    } else {
        ajaxOptions = {
            url : '/sessions',
            type : 'PUT',
            data : data,
            dataType : 'json',
            processData : false,
            success : onLoginSuccess,
            error : onLoginFailure,
        };
    }

    $.ajax(ajaxOptions);
};

var onLoginSuccess = function(data, status, xhr) {
};

var onLoginFailure = function(xhr, status, error) {
};

var onNewUserSuccess = function(data, status, xhr) {
};

var onNewUserFailure = function(xhr, status, error) {
};

var newUserDelegate = function(e) {
    $('#password-verify-input').fadeIn();
    $('#login').removeClass('primary').addClass('disabled');
    $('#new').removeClass('danger').addClass('disabled');

    $('#join').fadeIn(400, function() { 
        $('#login').fadeOut(1000);
        $('#new').fadeOut(1000);
    });
};

var init = function () {
    $('#password-verify-input').hide();
    $('#join').hide();
    $('#login').click(function(e) { 
        submitDelegate(e, false);
    });
    $('#join').click(function(e) { 
        submitDelegate(e, true);
    });
    $('#new').click(newUserDelegate);
};

