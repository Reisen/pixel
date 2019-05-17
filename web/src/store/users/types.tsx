export interface UserState {
    username: string;
    token?:   string;
}

export type State =
    | UserState
    | undefined;
