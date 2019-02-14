import React       from 'react';
import styles      from './IconButton.module.css';
import altStyles   from '../../utilities/altStyles';

interface Props {
    active?: boolean;
    icon?: string;
    letter?: string;
    onClick?: () => void;
}

const IconButton = (props: Props) => {
    return (
        <div onClick={props.onClick} className={`${styles.IconButton} ${props.active && styles.Active}`}>
            {props.icon}
        </div>
    );
};

export default IconButton;
