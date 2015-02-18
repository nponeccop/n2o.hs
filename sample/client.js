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

function onMessage(event) {
    if (event.data instanceof Blob)
    {
        $bert.on(event, function (x)
        {
            if (typeof x == 'string')
            {
                eval(x)
            }
            else
            {
                alert("Unknown x of " + typeof x)
            }
        })
        return
    }
    var p = $(document.createElement('p')).text(event.data);

    $('#messages').append(p);
    $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});

    if(event.data.match(/^[^:]* disconnected/)) {
        var user = event.data.replace(/ .*/, '');
        var idx = users.indexOf(user);
        users = users.slice(0, idx).concat(users.slice(idx + 1));
        refreshUsers();
    }
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
        var text = $('#text').val();
        ws.send(text);
        $('#text').val('');
        return false;
    });
    ws.onmessage = onMessage; 

    $('#join-form').submit(function () {
        $('#warnings').html('');
        ws.send(enc(tuple(atom('LOGON'),atom($('#user').val()))))
        $('#join').append('Connecting...');
        return false;
    });
});
