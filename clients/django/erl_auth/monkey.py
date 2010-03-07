from django.contrib import auth as django_auth
import auth

django_auth.login = auth.login
django_auth.logout = auth.logout
django_auth.authenticate = auth.authenticate
