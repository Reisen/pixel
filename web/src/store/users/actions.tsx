import 'whatwg-fetch';
import { Dispatch }                     from 'redux';
import { createAction }                 from 'redux-actions';
import { apiRegisterUser, apiAuthUser } from '../../api/users';

export const setUsername = createAction('SET_USERNAME')

export const registerUser =
    (email: string, password: string) =>
        async (dispatch: Dispatch) => {
            await apiRegisterUser({ email, password });
            dispatch(setUsername(email));
        };

export const loginUser =
    (email: string, password: string) =>
        async (dispatch: Dispatch) => {
            await apiAuthUser({ email, password });
            dispatch(setUsername(email));
        };
