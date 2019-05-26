import { State }         from './types';
import { handleActions } from 'redux-actions';
import { lensProp, set } from 'ramda';


const initialState: State = {
    username: 'Reisen',
};

export const userReducer = handleActions<State>({
    'SET_USERNAME':  (state, action) => set(
        lensProp('username'),
        action.payload
    )(state),

    'CLEAR_USERNAME': (state, action) => set(
        lensProp('username'),
        undefined
    )(state),
}, initialState);
