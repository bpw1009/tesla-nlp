from flask import Flask, render_template
from flask_wtf import FlaskForm
from wtforms import StringField, SubmitField
#from flask_images import resized_img_src

app = Flask(__name__,static_url_path='/static')


app.config['SECRET_KEY'] = 'testkey'

class InfoForm(FlaskForm):

    selection = StringField('Please type your selection')
    submit = SubmitField('Submit')


@app.route('/', methods=['GET','POST'])
def index():
    selection = False
    form = InfoForm()
    if form.validate_on_submit():
        selection = form.selection.data
        form.selection.data = ''
    return render_template('index.html', form=form, selection=selection)

if __name__ == '__main__':
    app.run(debug=False)
