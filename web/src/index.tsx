import React                              from 'react';
import thunk                              from 'redux-thunk';
import { BrowserRouter as Router, Route } from 'react-router-dom';
import { Provider }                       from 'react-redux';
import { reducers }                       from './store';
import { render }                         from 'react-dom';
import {
    applyMiddleware,
    combineReducers,
    createStore
}                                         from 'redux';

// Pages
import Index                              from './screens/Index/Index';
import ImageView                          from './screens/ImageView';
import ImageUpload                        from './screens/ImageUpload';
import ImageGalleries                     from './screens/ImageGalleries';

// Global Styles
import './App.css';
import 'normalize.css';
import './static/icofont/icofont.min.css';


// Create Global Store
const store = createStore(
    combineReducers(reducers),
    applyMiddleware(thunk)
);


// Main App
const App = () => (
    <Provider store={store}>
        <Router>
            <div className="dark">
                <Route exact path="/"                  component={Index} />
                <Route exact path="/my/upload"         component={ImageUpload} />
                <Route       path="/my/upload/:mode"   component={ImageUpload} />
                <Route exact path="/my/images/:page"   component={Index} />
                <Route exact path="/i/:uuid"           component={ImageView} />
                <Route       path="/i/:uuid/galleries" component={ImageGalleries} />
            </div>
        </Router>
    </Provider>
);


render(
    <App />,
    document.getElementById('root')
);
