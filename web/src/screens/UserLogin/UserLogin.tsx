// This Screen allows registration, the reason we do this instead of
// sharing the login screen/components is the goal to display information
// about the project during registration. This means the screens are more
// dissimilar than just a pair of forms.

import React            from 'react';
// import { User as user } from '../../api/types';
import { connect }      from 'react-redux';
import { State }        from '../../store';
import { History }      from 'history'
// import { registerUser } from '../../store/users';

// Components
import NavigationBar    from '../../components/NavigationBar';
import styles           from './UserRegister.module.css';


interface Props {
    registerUser: () => void;
    username:     string;
    history:      History;
}

const headerLinks = [
    {name: 'Login', path: ''},
    {name: 'Register', path: ''},
];

const UserRegister = (props: Props) => {
    return (
        <div className="Page">
            <NavigationBar links={headerLinks} username={props.username} />
            <div className={styles.UserRegister}>
                Foo Bar Baz I'm Registering yo Ass
            </div>
        </div>
    );
};

const mapState = (state: State) => ({
});

const mapDispatch = {
    // registerUser
};

export default connect(mapState, mapDispatch)(UserRegister);
