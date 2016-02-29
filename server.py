#!/usr/bin/env python3
import os
from bottle import static_file, route, run, template

__author__ = 'Remco Bloemen'

@route('/api/<name>')
def index(name):
	return template('<b>Hello {{name}}</b>!', name=name)

root = os.path.dirname(os.path.realpath(__file__)) + '/www'
@route('/<filepath:path>')
def server_static(filepath):
	'''Serve static files from local directory'''
	return static_file(filepath, root=root)

run(host='localhost', port=8080)
