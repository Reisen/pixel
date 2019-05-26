import { UserState, User } from './types';

export const getUser = (state: UserState): User => ({
    username: state.username,
    role:     'Member'
});
