export interface ApiState {
    base: string;
    api:  string;
}

export type State =
    | ApiState
    | undefined;
