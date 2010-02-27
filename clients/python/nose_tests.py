from nose.tools import *
from nose.failure import *
from erl_auth import *

def test_profile():
    p = Profile()
    #blank has no special meaning, it's just a string
    out = p.create('testuser', 'testpass', {'profile_item':'blank'})
    assert out == 'Created'
    
    profile = p.get('testuser')
    assert profile['profile_item'] == 'blank'

    out = p.update('testuser', {'profile_item':'no_longer_blank'})
    assert out == 'Changed'

    profile = p.get('testuser')
    assert profile['profile_item'] == 'no_longer_blank'
    
    out = p.delete('testuser')
    assert out == 'Deleted'
    
    try:
        assert_raises(p.get('testuser'), Exception)
    except Exception, e:
        assert str(e) == 'Error: Profile Not Found'

def auth_setup():
    p = Profile()
    p.create('testuser', 'testpass', {})

def auth_teardown():
    p = Profile()
    p.delete('testuser')

def test_auth():
    p = Profile()
    pas = Password()
    a = Auth()
    try: 
        assert_raises(a.authenticate('testuser', 'wrong_pass', 'testing'), Exception)
    except Exception, e:
        assert str(e) == 'Error: Bad Password'

    assert a.authenticate('testuser', 'testpass', 'testing')

    assert pas.change_password('testuser', 'newpass') == 'Changed'

    assert a.authenticate('testuser', 'newpass', 'testing')

    try:
        assert_raises(a.authenticate('testuser', 'testpass', 'testing'), Exception)
    except Exception, e:
        assert str(e) == 'Error: Bad Password'

test_auth.setup = auth_setup
test_auth.teardown = auth_teardown


def test_token():
    t = Token()
    a = Auth()
    token = a.authenticate('testuser', 'testpass', 'testing')
    token2 = a.authenticate('testuser', 'testpass', 'testing2')

    assert t.authenticate('testuser', token, 'testing') == 'Matched'
    assert t.authenticate('testuser', token2, 'testing2') == 'Matched'

    new_token = t.new('testuser', 'testing')
    t.delete('testuser', 'testing2')

    assert t.authenticate('testuser', new_token, 'testing') == 'Matched'
    try:
        assert_raises(t.authenticate('testuser', token, 'testing'))
    except Exception, e:
        assert str(e) == 'Error: Invalid Token'

    try:
        assert_raises(t.authenticate('testuser', token2, 'testing2'), Exception)
    except Exception, e:
        assert str(e)== 'Error: Invalid Token'
    
test_token.setup = auth_setup
test_token.teardown = auth_teardown
