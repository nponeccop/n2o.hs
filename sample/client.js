var users = [];

function refreshUsers() {
    $('#users').html('');
    for(i in users) {
        $('#users').append($(document.createElement('li')).text(users[i]));
    }
}

function addUser(user)
{
    users.push(user);
    refreshUsers();
}

function log(data)
{
    var p = $(document.createElement('p')).text(data);
    $('#messages').append(p);
    $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
}

function warning(data)
{
    $('#warnings').append(data);
    ws.close();
}

function joinSession()
{
    $('#join-section').hide();
    $('#chat-section').show();
    $('#users-section').show();
}

$(document).ready(function () {

    $('#message-form').submit(function () {
        ws.send(enc(tuple(atom('MSG'),bin($('#text').val()))))
        $('#text').val('');
        return false;
    });

    $('#join-form').submit(function () {
        $('#warnings').html('');
        ws.send(enc(tuple(atom('LOGON'),bin($('#user').val()))))
        $('#join').append('Connecting...');
        return false;
    });
});
