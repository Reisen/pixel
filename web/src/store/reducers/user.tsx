import { createAction, handleActions }             from 'redux-actions';

// -----------------------------------------------------------------------------

export interface UserState {
    username: string;
}

type State = UserState | undefined;

// -----------------------------------------------------------------------------

const initialState: UserState = {
    username: "Reisen"
};

// -----------------------------------------------------------------------------

// Pure Action for setting an authenticated user, this assumes the backend has
// already responded and authed the user and we just have a token and a rank
// capability to store.
interface LoginAction {
    username: string;
    token:    string;
}

export const login = createAction<LoginAction>('LOGIN');

// Un-authing, currently, involves just deleting the data from the store. There
// is no back-end enforcement.
export const logout      = createAction('LOGOUT');

export const userReducer = handleActions<State>(
    { },
    initialState
);
