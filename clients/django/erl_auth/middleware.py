from django.conf import settings
import erl_auth

ERL_SESSION_KEY = 'erl_auth_username'
ERL_SESSION_TOKEN = 'erl_auth_token'

import auth

class AuthMiddleware(object):
    _patched = False
    def process_request(self, request):
        username = request.COOKIES.get(ERL_SESSION_KEY, None)
        token = request.COOKIES.get(ERL_SESSION_TOKEN, None)
        user = auth.authenticate(username=username, token=token) if username and token else None
        if user:
            request.user = user
            auth.login(request, user)
        else:
            request.user = auth.AnonymousUser()
        
    def process_response(self, request, response):
        #every request/response is cycled with a new token
        user = getattr(request, 'user', None)
        if user and user.is_authenticated() and user._token:
            response.set_cookie(ERL_SESSION_KEY, user.username)
            t = erl_auth.Token()
            project = getattr(settings, "PROJECT_NAME", "django")
            new_token = t.new(user.username, project)
            response.set_cookie(ERL_SESSION_TOKEN, new_token)
        elif not user:
            response.delete_cookie(ERL_SESSION_KEY)
            response.delete_cookie(ERL_SESSION_TOKEN)
        return response

