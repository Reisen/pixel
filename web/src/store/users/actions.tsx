import 'whatwg-fetch';
import { Dispatch }                     from 'redux';
import { createAction }                 from 'redux-actions';
import { apiRegisterUser, apiAuthUser } from '../../api/users';

// Authorization methods return a token used for bearer token requests, so
// we need an action to update the state with this.
export const setToken = createAction('SET_TOKEN')

// Registering a user returns a brand new bearer token to set.
export const registerUser =
    (email: string, password: string) =>
        async (dispatch: Dispatch) => {
            const token = await apiRegisterUser({
                email,
                password
            });

            dispatch(setToken(token.text));
        };

// Authorizing a user returns an existing bearer token to set.
export const loginUser =
    (email: string, password: string) =>
        async (dispatch: Dispatch) => {
            const token = await apiAuthUser({
                email,
                password
            });

            dispatch(setToken(token.text));
        };
