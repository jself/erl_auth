import erl_auth
from django.contrib.auth.models import User, AnonymousUser
from django.conf import settings

def get_or_create_user(username):
    p = erl_auth.Profile()
    try:
        profile = p.get(username)
    except:
        profile = {}
    try:
        u = User.objects.get(username=username)
    except User.DoesNotExist:
        u = User(username=username)
        u.save()
    changed = False
    for item in profile:
        if getattr(u, item, None) != profile[item]:
            setattr(u, item, profile[item])
        setattr(u, item, profile[item])
        u.save()
    return u

class AuthBackend:
    supports_object_permissions = False
    supports_anonymous_user = True
    def authenticate(self, username=None, password=None, token=None):
        t = erl_auth.Token()
        a = erl_auth.Auth()

        project = getattr(settings, "PROJECT_NAME", "django")

        if token and username:
            try:
                if t.authenticate(username, token, project):
                    user = get_or_create_user(username)
                    user.token = token
                    return user
            except:
                #token did not work, fall back to password
                pass

        if username and password:
            try:
                token = a.authenticate(username, password, project)
                user = get_or_create_user(username)
                user.token = token
                return user
            except:
                pass

        return None

    def get_user(self, user_id):
        try:
            return User.objects.get(pk=user_id)
        except User.DoesNotExist:
            return None()

