// This Screen allows registration, the reason we do this instead of
// sharing the login screen/components is the goal to display information
// about the project during registration. This means the screens are more
// dissimilar than just a pair of forms.

import React            from 'react';
// import { User as user } from '../../api/types';
import { connect }      from 'react-redux';
import { State }        from '../../store';
import { History }      from 'history'
import { loginUser }    from '../../store/users';

// Components
import NavigationBar    from '../../components/NavigationBar';
import TextInput        from '../../components/TextInput';
import Button           from '../../components/Button';
import styles           from './UserLogin.module.css';


interface Props {
    loginUser: (email: string, password: string) => void;
    username:  string;
    history:   History;
}

const headerLinks = [
    {name: 'Login', path: ''},
    {name: 'Register', path: ''},
];

const UserLogin = (props: Props) => {
    const [email, setEmail]       = React.useState('');
    const [password, setPassword] = React.useState('');
    const [waiting, setWaiting]   = React.useState(true);

    // Handle Registration Clicks
    const handleCreate = () => {
        setWaiting(true);
        props.loginUser(email, password);
    };

    return (
        <div className="Page">
            <NavigationBar links={headerLinks} username={props.username} />
            <div className={styles.Root}>
                <div className={styles.RegisterForm}>
                    <TextInput
                        onChange={e => setEmail(e.target.value)}
                        placeholder="Email"
                        value={email}
                    />

                    <TextInput
                        onChange={e => setPassword(e.target.value)}
                        placeholder="Password"
                        type="password"
                        value={password}
                    />

                    <Button disabled={waiting} onClick={handleCreate}>
                        Create My Account
                    </Button>
                </div>
            </div>
        </div>
    );
};

const mapState = (state: State) => ({
});

const mapDispatch = {
    loginUser
};

export default connect(mapState, mapDispatch)(UserLogin);
