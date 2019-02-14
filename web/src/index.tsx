import Index                    from './screens/Index/Index';
import Image                    from './screens/Image/Image';
import React, { Component }     from 'react';
import { render }               from 'react-dom';
import { HashRouter as BrowserRouter, Route } from 'react-router-dom';

import './App.css';
import 'normalize.css';

/* -------------------------------------------------------------------------- */

const App = () => (
    <BrowserRouter>
        <div>
            <Route exact path="/" component={Index} />
            <Route       path="/i/:uuid" component={Image} />
        </div>
    </BrowserRouter>
);

/* -------------------------------------------------------------------------- */

render(<App />, document.getElementById('root'));
