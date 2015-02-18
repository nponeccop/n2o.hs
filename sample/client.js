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

$(document).ready(function () {
    $('#join-form').submit(function () {
        $('#warnings').html('');
        var user = $('#user').val();

        ws.send(enc(tuple(atom('LOGON'),atom(user))))

        ws.onmessage = function(event) {
            if(event.data.match('^Welcome! Users: ')) {
                /* Calculate the list of initial users */
                var str = event.data.replace(/^Welcome! Users: /, '');
                if(str != "") {
                    users = str.split(", ");
                    refreshUsers();
                }

                $('#join-section').hide();
                $('#chat-section').show();
                $('#users-section').show();

                ws.onmessage = onMessage;

                $('#message-form').submit(function () {
                    var text = $('#text').val();
                    ws.send(text);
                    $('#text').val('');
                    return false;
                });
            } else {
                $('#warnings').append(event.data);
                ws.close();
            }
        };

        $('#join').append('Connecting...');

        return false;
    });
});
