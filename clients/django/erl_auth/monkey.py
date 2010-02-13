from django.contrib import auth
import datetime
from middleware import ERL_SESSION_KEY, ERL_SESSION_TOKEN
from django.conf import settings

def login(request, user):
    """
    Persist a user id and a backend in the request. This way a user doesn't
    have to reauthenticate on every request.
    """
    if user is None:
        user = request.user
    # TODO: It would be nice to support different login methods, like signed cookies.
    user.last_login = datetime.datetime.now()
    user.save()

    if auth.SESSION_KEY in request.session:
        if request.session[auth.SESSION_KEY] != user.id:
            # To avoid reusing another user's session, create a new, empty
            # session if the existing session corresponds to a different
            # authenticated user.
            request.session.flush()
    else:
        request.session.cycle_key()

    request.session[auth.SESSION_KEY] = user.id
    request.session[auth.BACKEND_SESSION_KEY] = user.backend

    request.COOKIES[ERL_SESSION_KEY] = user.username
    request.COOKIES[ERL_SESSION_TOKEN] = user.token


    request.user = user

def logout(request):
    """
    Removes the authenticated user's ID from the request and flushes their
    session data.
    """
    request.session.flush()
    if hasattr(request, 'user'):
        user = getattr(request, 'user')
        from django.contrib.auth.models import AnonymousUser
        request.user = AnonymousUser()

        import erl_auth
        t = erl_auth.Token()
        project = getattr(settings, "PROJECT_NAME", "django")
        t.delete(user.username, project)

auth.login = login
auth.logout = logout
