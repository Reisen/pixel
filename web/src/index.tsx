import { reducers }                     from './store';
import Image                            from './screens/Image/Image';
import Index                            from './screens/Index/Index';
import React                            from 'react';
import { HashRouter as Router, Route }  from 'react-router-dom';
import { Provider }                     from 'react-redux';
import { createStore, combineReducers } from 'redux';
import { render }                       from 'react-dom';

import './App.css';
import 'normalize.css';

/* -------------------------------------------------------------------------- */

const store = createStore(combineReducers(reducers));

/* -------------------------------------------------------------------------- */

const App = () => (
    <Provider store={store}>
        <Router>
            <div className="dark">
                <Route exact path="/" component={Index} />
                <Route       path="/i/:uuid" component={Image} />
            </div>
        </Router>
    </Provider>
);

/* -------------------------------------------------------------------------- */

render(<App />, document.getElementById('root'));
