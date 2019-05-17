import { State }         from './types';
import { handleActions } from 'redux-actions';
import { lensProp, set } from 'ramda';


const initialState: State = {
    api:  'localhost:3001',
    base: 'localhost:3001',
};

export const apiReducer = handleActions<State>({
    'SET_API':  (state, action) => set(lensProp('api'), action.payload)(state),
    'SET_BASE': (state, action) => set(lensProp('base'), action.payload)(state),
}, initialState);
