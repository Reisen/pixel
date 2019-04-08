import { State }         from './types';
import { handleActions } from 'redux-actions';


const initialState: State = {
    api:  'localhost:3001',
    base: 'localhost:3001',
};

export const apiReducer = handleActions<State>({
    'SET_API':  (state, action) => state,
    'SET_BASE': (state, action) => state,
}, initialState);
