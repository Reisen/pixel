import 'whatwg-fetch';
import Cookies from 'universal-cookie';

// These Types are automatically generated from the back-end, if the back-end
// updates this file should then automatically break if it goes out of sync
// with the API.
import {
    AuthUserRequest,
    RegisterRequest,
    Token,
} from './types';


// HACK, BIG HACK
const findApiBase = () => {
    const cookies = new Cookies();
    return cookies.get('api') || 'http://localhost:3001';
};

const apiRegisterUser = async (req: RegisterRequest): Promise<Token> => {
    const base = findApiBase();
    const request = {
        headers: { 'Content-Type': 'application/json' },
        method:  'POST',
        body:     JSON.stringify(req),
    };

    const response = await fetch(`${base}/api/user`, request)
    const json     = await response.json();
    return json;
};

const apiAuthUser = async (req: AuthUserRequest): Promise<Token> => {
    const base = findApiBase();
    const request = {
        headers: { 'Content-Type': 'application/json' },
        method:  'POST',
        body:     JSON.stringify(req),
    };

    const response = await fetch(`${base}/api/user/login`, request)
    const json     = await response.json();
    return json;
}

export {
    apiAuthUser,
    apiRegisterUser,
}

