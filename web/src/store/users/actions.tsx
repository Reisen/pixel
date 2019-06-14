import 'whatwg-fetch';
import Cookies                     from 'universal-cookie';
import { AxiosResponse }           from 'axios';
import { Dispatch }                from 'redux';
import { createAction }            from 'redux-actions';
import {
    registerUser as apiRegisterUser,
    loginUser    as apiLoginUser,
}                                  from '../../Api';

export const setUsername   = createAction('SET_USERNAME');
export const clearUsername = createAction('CLEAR_USERNAME');

// Hit the backend register API endpoint and set the
// username with the result.
//
// TODO: Better error handling story. Perhaps something
// where a global error state is set and components can
// easily opt in to specific error states.
export const registerUser =
    (email: string, password: string) =>
        async (dispatch: Dispatch): Promise<AxiosResponse> => {
            const response = await apiRegisterUser({ email, password });
            dispatch(setUsername(email));
            return response;
        };

// Hit the backend login API endpoint and set the
// username with the result.
export const loginUser =
    (email: string, password: string) =>
        async (dispatch: Dispatch): Promise<AxiosResponse> => {
            const response = await apiLoginUser({ email, password });
            dispatch(setUsername(email));
            return response;
        };

// Delete token cookie and dispatch an action to clear
// the stored user data.
export const logoutUser = () =>
    async (dispatch: Dispatch): Promise<void> => {
        const cookies = new Cookies();
        cookies.remove('token');
        dispatch(clearUsername());
    };
