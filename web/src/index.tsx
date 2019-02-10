import Index                    from './screens/Index/Index';
import React, { Component }     from 'react';
import { render }               from 'react-dom';
import { BrowserRouter, Route } from 'react-router-dom';

import './App.css';

/* -------------------------------------------------------------------------- */

const App = () => (
    <BrowserRouter>
        <Route exact path="/" component={Index} />
    </BrowserRouter>
);

/* -------------------------------------------------------------------------- */

render(<App />, document.getElementById('root'));
