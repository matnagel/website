from flask import Flask, send_from_directory

app = Flask(__name__)

@app.route("/")
def index():
    return send_from_directory("content", "index.html")

@app.route("/cert")
def cert():
    return send_from_directory("content", "cert.html")

@app.route('/<path:path>')
def get_content(path):
    return send_from_directory('content', path)

# If `entrypoint` is not defined in app.yaml, App Engine will look for an app
# called `app` in `main.py`.

if __name__ == '__main__':
    # This is used when running locally only. When deploying to Google App
    # Engine, a webserver process such as Gunicorn will serve the app. This
    # can be configured by adding an `entrypoint` to app.yaml.
    app.run(host='127.0.0.1', port=8080, debug=True)
