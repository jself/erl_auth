from django.contrib import auth
from django.contrib.auth.models import AnonymousUser
from django.conf import settings
import erl_auth
ERL_SESSION_KEY = 'erl_auth_username'
ERL_SESSION_TOKEN = 'erl_auth_token'

class AuthMiddleware(object):
    def process_request(self, request):
        username = request.COOKIES.get(ERL_SESSION_KEY, None)
        token = request.COOKIES.get(ERL_SESSION_TOKEN, None)
        user = auth.authenticate(username=username, token=token) if username and token else None
        if user:
            request.user = user
            auth.login(request, user)
        else:
            request.user = AnonymousUser()
        
    def process_response(self, request, response):
        user = getattr(request, 'user', None)
        if user and user.is_authenticated() and hasattr(user, 'token'):
            response.set_cookie(ERL_SESSION_KEY, user.username)
            t = erl_auth.Token()
            project = getattr(settings, "PROJECT_NAME", "django")
            new_token = t.new(user.username, project)
            response.set_cookie(ERL_SESSION_TOKEN, new_token)
        elif not user:
            response.delete_cookie(ERL_SESSION_KEY)
            response.delete_cookie(ERL_SESSION_TOKEN)
        return response

