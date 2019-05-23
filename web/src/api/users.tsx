import 'whatwg-fetch';
import Cookies from 'universal-cookie';

// These Types are automatically generated from the back-end, if the back-end
// updates this file should then automatically break if it goes out of sync
// with the API.
import {
    AuthUserRequest,
    RegisterRequest,
} from './types';


// HACK, BIG HACK
const findApiBase = () => {
    const cookies = new Cookies();
    return cookies.get('api') || 'http://localhost:3001';
};

const apiRegisterUser = async (req: RegisterRequest): Promise<Response> => {
    const base = findApiBase();
    const request = {
        body:         JSON.stringify(req),
        credentials: 'include' as 'include',
        headers:     { 'Content-Type': 'application/json' },
        method:      'POST',
    };

    const result = await fetch(`${base}/api/user`, request);
    console.log(result);
    result.headers.forEach(header => {
        console.log(header);
    });
    return result;
};

const apiAuthUser = async (req: AuthUserRequest): Promise<Response> => {
    const base = findApiBase();
    const request = {
        body:         JSON.stringify(req),
        credentials: 'include' as 'include',
        headers:     { 'Content-Type': 'application/json' },
        method:      'POST',
    };

    return fetch(`${base}/api/user/login`, request);
}

export {
    apiAuthUser,
    apiRegisterUser,
}

