from middleware import ERL_SESSION_KEY, ERL_SESSION_TOKEN
from django.conf import settings
from models import User, AnonymousUser
import erl_auth

def get_user(username):
    p = erl_auth.Profile()
    try:
        profile = p.get(username)
    except:
        return None
    profile['username'] = username
    return User(**dict([(str(k), v) for k, v in profile.iteritems()]))

def authenticate(username, password=None, token=None):

    t = erl_auth.Token()
    a = erl_auth.Auth()

    project = getattr(settings, "PROJECT_NAME", "django")

    if token and username:
        try:
            if t.authenticate(username, token, project):
                user = get_user(username)
                user._token = token
                return user
        except:
            #token did not work, fall back to password
            pass
    
    if username and password:
        try:
            token = a.authenticate(username, password, project)
            user = get_user(username)
            user._token = token
            return user
        except:
            pass

    return None


def login(request, user):
    project = getattr(settings, "PROJECT_NAME", "django")
    t = erl_auth.Token()
    if not user.username or not user.is_authenticated():
        raise Exception("User must be authenticated to log in")
    token = user._token or t.new(user.username, project)
    user._token = token
    request.COOKIES[ERL_SESSION_KEY] = user.username
    request.COOKIES[ERL_SESSION_TOKEN] = token
    request.user = user

def logout(request):
    project = getattr(settings, "PROJECT_NAME", "django")
    user = getattr(request, 'user', None)
    if not user:
        request.user = AnonymousUser()
        return None
    t = erl_auth.Token()
    if not user.username or not user.is_authenticated():
        return
    t.delete(user.username, project)
    request.COOKIES[ERL_SESSION_KEY] = None
    request.COOKIES[ERL_SESSION_TOKEN] = None
    request.user = AnonymousUser()

