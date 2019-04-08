import React       from 'react';
import styles      from './Toggle.module.css';


interface Props {
    on: boolean;
    onPress: (current: boolean) => void;
    text: string;
}

const Toggle = (props: Props) => (
    <div className={styles.Toggle}>
        <div className={styles.Toggler}>
            <div className={styles.ToggleBlock}>
            </div>
        </div>

        <span>{props.text}</span>
    </div>
);


export default Toggle;
