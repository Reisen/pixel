import 'whatwg-fetch';
import { Dispatch }                     from 'redux';
import { createAction }                 from 'redux-actions';
import { apiRegisterUser, apiAuthUser } from '../../api/users';

export const setUsername = createAction('SET_USERNAME')

export const registerUser =
    (email: string, password: string) =>
        async (dispatch: Dispatch): Promise<Response> => {
            const response = await apiRegisterUser({ email, password });
            dispatch(setUsername(email));
            return response;
        };

export const loginUser =
    (email: string, password: string) =>
        async (dispatch: Dispatch): Promise<Response> => {
            const response = await apiAuthUser({ email, password });
            dispatch(setUsername(email));
            return response;
        };
