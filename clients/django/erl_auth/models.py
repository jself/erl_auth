import erl_auth

class OpenDict(object):
    def __init__(self, *args, **kwargs):
        self.__dict__['_d'] = {}
        self._d.update(kwargs)

    def __getattr__(self, name):
        return self._d.get(name, None)

    def __setattr__(self, name, value):
        if name in self.__dict__ or name.startswith('_'):
            self.__dict__[name] = value
        else:
            self._d[name] = value

    def __delattr__(self, name):
        if name in self._d[name]:
            del self._d[name]
        else:
            super(OpenDict, self).__delattr__(self, name)


class AnonymousUser(OpenDict):
    def is_authenticated(self):
        return False

class User(OpenDict):
    """
    The user object is an open dict based on values passed from 
    the erl_auth backend. It impliments some methods from Django's
    User class.
    """
    def __init__(self, *args, **kwargs):
        self._is_new = args[0] if args else kwargs.pop('is_new', False)
        super(User, self).__init__(*args, **kwargs)

    def is_authenticated(self):
        return True

    def is_new(self):
        return self._is_new
    
    def __getattr__(self, name):
        val = self._d.get(name, None)
        if val:
            return val
        if name.startswith('_'):
            return None
        username = self._d.get('username', None)
        if not username:
            return None
        return getattr(self.get_django_user(username), name)

    def get_django_user(self, username):
        user = getattr(self, '_django_user', None)
        if user:
            return user
        from django.contrib.auth.models import User
        changed = False
        try:
            user = User.objects.get(username=username)
        except User.DoesNotExist:
            user = User(**dict((str(k), v) for k, v in self._d.iteritems()))
            changed = True
        for k, v in self._d.iteritems():
            if getattr(user, k, None) != v:
                setattr(user, k, v)
                changed = True
        if changed:
            user.save()
        self._django_user = user
        return user

    def save(self):
        username = self._d.pop('username', None)
        if not username:
            raise Exception("Username required to save")
        p = erl_auth.Profile()
        if self.is_new:
            password = self._d.pop('password', None)
            if not password:
                raise Exception('Password required for new user.')
            p.create(username, password, self._d)
        else:
            p.update(username, self._d)

    def set_password(self, password):
        username = self._d.pop('username', None)
        if not username:
            raise Exception('Username required to change password')
        p = erl_auth.Password()
        p.change_password(username, password)
