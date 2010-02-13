try:
    import httplib2
except:
    print "httplib2 is required to run this client. It can be downloaded from http://code.google.com/p/httplib2/"
    import sys
    sys.exit(-1)
import urllib
import json
url = 'http://localhost:8000'

class RestClient(object):
    def __init__(self):
        self.http = httplib2.Http()

    def _get(self, *args, **kwargs):
        resp, content = self.http.request(*args, **kwargs)
        if resp['status'] != '200':
            raise Exception("Bad response code %s"%resp['status'])
        if content.startswith('Error:'):
            raise Exception(content)
        return content

class Profile(RestClient):
    def __init__(self):
        super(Profile, self).__init__()
        self.url = url + "/profile"
    def get(self, username):
        args = urllib.urlencode({'username':username})
        content = self._get(self.url + "?" + args, "GET")
        profile = json.loads(content)
        return json.loads(profile['profile'])
    def create(self, username, password, profile):
        profile = json.dumps(profile)
        args = urllib.urlencode(dict(username=username, password=password, profile=profile))
        content = self._get(self.url, "POST", body=args)
        return content
    def update(self, username, profile):
        profile = json.dumps(profile)
        args = urllib.urlencode(dict(username=username, profile=profile))
        content = self._get(self.url, "POST", body=args)
        return content
    def delete(self, username):
        args = urllib.urlencode({'username':username})
        content = self._get(self.url, "DELETE", body=args)
        return content

class Auth(RestClient):
    def __init__(self):
        super(Auth, self).__init__()
        self.url = url + '/auth'
    def authenticate(self, username, password, application):
        args = urllib.urlencode(dict(username=username, password=password, application=application))
        content = self._get(self.url, "POST", body=args)
        return content

class Token(RestClient):
    def __init__(self):
        super(Token, self).__init__()
        self.url = url + "/token"

    def authenticate(self, username, token, application):
        args = urllib.urlencode(dict(username=username, token=token, application=application))
        content = self._get(self.url, "POST", body=args)
        return content

    def delete(self, username, application):
        args = urllib.urlencode(dict(username=username, application=application))
        content = self._get(self.url, "DELETE", body=args)
        return content

