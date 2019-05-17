import { State }         from './types';
import { handleActions } from 'redux-actions';
import { lensProp, set } from 'ramda';


const initialState: State = {
    username: 'Reisen',
    token:    undefined
};

export const userReducer = handleActions<State>({
    'SET_TOKEN':  (state, action) => {
        alert('Token Set');
        return set(lensProp('token'), action.payload)(state);
    }
}, initialState);
