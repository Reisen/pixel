import React       from 'react';
import styles      from './IconButton.module.css';

interface Props {
    active?: boolean;
    icon?: string;
    letter?: string;
    onClick?: () => void;
}

const IconButton = (props: Props) => {
    return (
        <div
            onClick={props.onClick}
            className={props.active ? styles.Root__active : styles.Root} >

            {
                props.icon
                    ? <i className={`icofont-${props.icon}`}></i>
                    : props.letter
            }
        </div>
    );
};

export default IconButton;
