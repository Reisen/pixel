// Define the type of the User reducer state.
export interface UserState {
    username: string;
}

// Wrap it up in an optional to make the type checker
// happy.
export type State =
    | UserState
    | undefined;

// The type a User is represented as within the
// reducer state.
export interface User {
    username: string;
    role:     string;
}
