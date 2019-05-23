import React                              from 'react';
import thunk                              from 'redux-thunk';
import storage                            from 'redux-persist/lib/storage';
import { persistStore, persistReducer }   from 'redux-persist';
import { PersistGate }                    from 'redux-persist/integration/react';
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
import UserRegister                       from './screens/UserRegister';
import UserLogin                          from './screens/UserLogin';

// Global Styles
import 'normalize.css';
import './static/icofont/icofont.min.css';
import './App.css';


// Create Redux-Persist Configuration, we use this so that when viewing pages
// such as image pages, we can show the image from cached state without having
// to wait for the server to send us details.
const persistConfig = {
    key: 'root',
    storage,
};

const store = createStore(
    persistReducer(persistConfig, combineReducers(reducers)),
    applyMiddleware(thunk)
);

// Used to Write/Read redux state to an offline store.
const persistor = persistStore(store);


// Main App
const App = () => (
    <Provider store={store}>
        <PersistGate loading={null} persistor={persistor}>
        <Router>
            <div className="dark">
                <Route exact path="/"                  component={Index} />

                {/* User Routes */}
                <Route exact path="/my/register"       component={UserRegister} />
                <Route exact path="/my/login"          component={UserLogin} />

                {/* Image Routes */}
                <Route exact path="/my/upload"         component={ImageUpload} />
                <Route       path="/my/upload/:mode"   component={ImageUpload} />
                <Route exact path="/my/images/:page"   component={Index} />
                <Route exact path="/i/:uuid"           component={ImageView} />
                <Route       path="/i/:uuid/galleries" component={ImageGalleries} />
            </div>
        </Router>
        </PersistGate>
    </Provider>
);


render(
    <App />,
    document.getElementById('root')
);
