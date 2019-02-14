export interface UserState {
    username: string;
}

const initialState: UserState = {
    username: "Reisen"
};

export const userReducer = (state: UserState = initialState, action: any) => {
    return state;
};
