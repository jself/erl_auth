import erl_auth
import auth
from django.conf import settings

class AuthBackend:
    supports_object_permissions = False
    supports_anonymous_user = True
    def authenticate(self, username=None, password=None, token=None):
        return auth.authenticate(username, password, token)
