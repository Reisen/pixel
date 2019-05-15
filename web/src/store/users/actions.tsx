import 'whatwg-fetch';
import { Dispatch }        from 'redux';
import { apiRegisterUser } from '../../api/users';


export const registerUser =
    (email: string, password: string) =>
        async (dispatch: Dispatch) => {
            await apiRegisterUser({
                email,
                password
            });
        };
