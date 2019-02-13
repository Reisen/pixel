import React       from 'react';
import styles      from './IconButton.module.css';
import altStyles   from '../../utilities/altStyles';

interface Props {
    icon: string;
    active?: boolean;
}

const IconButton = (props: Props) => {
    return (
        <div className={`${styles.IconButton} ${props.active && styles.Active}`}>
            {props.icon}
        </div>
    );
};

export default IconButton;
