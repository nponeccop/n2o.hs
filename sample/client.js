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
            else if (typeof x == 'object'
                && x.t == 104
                && x.v.length == 3
                && x.v[0].v == 'io') 
            {

                var foo = x.v[0]
                var data = foo[1]
                eval(x.v[2].v)
            }
            else
            {
                alert("Unknown x of " + typeof x)
                console.log(x)
            }
        })
    }
    else
    {
        alert("Non-blob data")
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
        ws.send(enc(tuple(atom('MSG'),bin($('#text').val()))))
        $('#text').val('');
        return false;
    });
    ws.onmessage = onMessage; 

    $('#join-form').submit(function () {
        $('#warnings').html('');
        ws.send(enc(tuple(atom('LOGON'),bin($('#user').val()))))
        $('#join').append('Connecting...');
        return false;
    });
});
